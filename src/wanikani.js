#!/usr/bin/env node
const fs = require('fs').promises;

const axios = require('axios');
const yargs = require('yargs');
const YAML = require('yaml');
const inquirer = require('inquirer').default;
const { hideBin } = require('yargs/helpers');


const WANIKANI_API_KEY = process.env.WANIKANI_API_KEY;
if (!WANIKANI_API_KEY) {
  console.error('Please set WANIKANI_API_KEY in your .env file');
  return;
}
const HEADERS = {
  'Authorization': `Bearer ${WANIKANI_API_KEY}`,
  'Wanikani-Revision': '20170710'
};

const RE_KANJI = /\p{Unified_Ideograph}/ug;
const RE_KATA = /\p{sc=Katakana}/u;
const RE_KANJI_NUM = /^第?[一二三四五六七八九十]+/u;


/**
 * Fetches a list of subjects from WaniKani. See here: https://docs.api.wanikani.com/20170710/#get-all-subjects
 * @param {string} types 
 * @returns An array of Subject objects
 */
async function getSubjects(params) {
  let response = await axios.get('https://api.wanikani.com/v2/subjects', {
    headers: HEADERS,
    params
  });
  const subjects = response.data.data;
  while (response.data.pages.next_url != null) {
    //console.log(response.data.pages.next_url);
    response = await axios.get(response.data.pages.next_url, {
      headers: HEADERS,
    });
    subjects.push(...response.data.data);
  }
  return subjects;
}


/**
 * Fetches a list of assignments from WaniKani. See here: https://docs.api.wanikani.com/20170710/#get-all-assignments
 * @param {object} params The parameters to be passed to the endpoint.
 * @returns list of assignments
 */
async function getAssignments(params) {
  let response = await axios.get('https://api.wanikani.com/v2/assignments', {
    headers: HEADERS,
    params
  });
  const assignments = response.data.data;
  while (response.data.pages.next_url != null) {
    response = await axios.get(response.data.pages.next_url, {
      headers: HEADERS,
    });
    assignments.push(...response.data.data);
  }
  return assignments;
}


async function startAssignment(assignment_id) {
  return await axios.put(`https://api.wanikani.com/v2/assignments/${assignment_id}/start`,
    {assignment: {}},
    {
      headers: {
        'Authorization': `Bearer ${WANIKANI_API_KEY}`,
        'Wanikani-Revision': '20170710',
        'Content-Type': 'application/json'
      }
    }
  );
}


async function getJLPTVocab(maxLevel) {
  const RE_DISAMBIG = /-\d$/;
  const vocab = JSON.parse(await fs.readFile('jlpt-n5.json', 'utf8'));
  if (maxLevel <= 4) {
    vocab.push(...JSON.parse(await fs.readFile('jlpt-n4.json', 'utf8')));
  }
  if (maxLevel <= 3) {
    vocab.push(...JSON.parse(await fs.readFile('jlpt-n3.json', 'utf8')));
  }
  for (const v of vocab) {
    const sense1 = v.senses[0];
    v.reading = v.japanese[0].reading;
    if (sense1.tags.indexOf('Usually written using kana alone') !== -1) {
      v.slug = v.reading;
      continue;
    } else if (v.japanese.length == 1 && v.japanese[0].word === undefined) {
      v.slug = v.reading;
      continue;
    }
    
    if (v.slug.match(RE_DISAMBIG)) {
      v.slug = v.slug.substring(0, v.slug.length-2)
    }
  }
  return vocab;
}


/**
 * Counts the number of assignments at each review level. For example, 1-4 ~ Apprentice I-IV.
 * @returns An object that maps srs_stage -> counts.
 */
async function getAssignmentsByLevel() {
  const assignments = await getAssignments({
    started: true
  });
  
  const assignmentsByLevel = {};
  assignments.forEach(a => {
    if (a.data.srs_stage in assignmentsByLevel) {
      assignmentsByLevel[a.data.srs_stage] += 1;
    } else {
      assignmentsByLevel[a.data.srs_stage] = 1;
    }
  });
  return assignmentsByLevel;
}


/**
 * Moves vocabulary from JLPT which is available for lessons into review.
 */
async function moveJLPTVocabToReview(numItems, maxJLPTLevel) {
    try {
      // Fetch all subjects
      const assignments = await getAssignments({
        unlocked: true,
        immediately_available_for_lessons: true,
      });
  
      const lessonSubjectIds = {};
      assignments.forEach(a => {
        lessonSubjectIds[a.data.subject_id] = a;
      });

      const subjects = await getSubjects({types: 'vocabulary,kana_vocabulary'});
      
      const N5toN4Words = new Set((await getJLPTVocab(maxJLPTLevel)).map(v => v.slug));

      const vocabToReview = subjects.filter(subject => {
        // Skip if already reviewed
        if (lessonSubjectIds[subject.id] === undefined) return false;

        if (subject.data.type === 'kana_vocabulary') return true;
        
        let chars = subject.data.characters;
        if (chars.startsWith('〜')) {
          chars = chars.substring(1);
        }
        const m = chars.match(RE_KANJI_NUM);
        if (m) {
          chars = chars.substring(m[0].length);
        }
        if (chars.endsWith('する')) {
          chars = chars.substring(0, chars.length - 2);
        }
        if (chars.endsWith('に')) {
          chars = chars.substring(0, chars.length - 1);
        }
        if (N5toN4Words.has(chars)) return true;
        if (chars.startsWith('お')) {
          if (N5toN4Words.has(chars.substring(1))) return true;
        } else {
          if (N5toN4Words.has('お' + chars)) return true;
        }

        return false;
      }).map(subject => ({
        assignment: lessonSubjectIds[subject.id],
        subject: subject,
      }));

      // Move each vocab to review
      const numToMove = Math.min(vocabToReview.length, numItems);
      for (let i=0; i < numToMove; i++) {
        const vocab = vocabToReview[i];
        await startAssignment(vocab.assignment.id);
        console.log(`Moved ${vocab.subject.data.characters} (Level ${vocab.subject.data.level}) to review`);
        console.log(vocab.subject.data);
      }
  
      console.log(`Total vocabulary moved to review: ${numToMove}`);
      if (numToMove !== vocabToReview.length) {
        console.log(`Not moved: ${vocabToReview.length-numToMove}`);
      }
  
    } catch (error) {
      console.error('Error moving vocabulary to review:', 
        error.response ? error.response.data : error.message
      );
      //console.log(error.response.config);
    }
}


async function showComprehensibleVocab() {
  const assignments = await getAssignments({
    started: true,
  });
  
  const lessonSubjectIds = {};
  assignments.forEach(a => {
    lessonSubjectIds[a.data.subject_id] = a;
  });
  
  const learnedKanji = new Set((await getSubjects({types: 'kanji'})).filter(s => {
    return lessonSubjectIds[s.id] !== undefined;
  }).map(s => {
    return s.data.characters;
  }));

  const learnableVocab = new Set((await getSubjects({types: 'vocabulary,kana_vocabulary'})).map(s => {
    return s.data.characters;
  }));

  const leftover = (await getJLPTVocab()).filter(v => {
    if (learnableVocab.has(v.slug)) return false;
    
    for (const m of v.slug.matchAll(RE_KANJI)) {
      if (!learnedKanji.has(m[0])) {
        return false;
      }
    }

    return true;
  });

  for (const v of leftover) {
    console.log(v.japanese.length === 1 ? v.japanese.length[0] : v.japanese);
  }
  process.exit(0);
}


// create custom vocabulary list in optimal order
// 1. katakana and hiragana-only words
// 2. words not in wanikani by increasing level (max level of kanji)
// 3. words in wanikani by decreasing level (level of vocab)
// TODO: add all unlocked words to review in wanikani
// TODO: custom sources (for now, it is just JLPT vocab)
// TODO: blacklist
async function customVocabList() {
  const kanji = JSON.parse(await fs.readFile('kanji.json', 'utf8'));;
  const kanjiByWaniKaniLevel = {};
  let maxLevel = 0;
  for (const k in kanji) {
    kanjiByWaniKaniLevel[k] = kanji[k].wk_level;
    maxLevel = Math.max(maxLevel, kanji[k].wk_level);
  }

  const subjects = await getSubjects({types: 'vocabulary,kana_vocabulary'});
  const wkVocab = {};
  for (const s of subjects) {
    wkVocab[s.data.characters] = s;
  }

  const vocab = (await getJLPTVocab()).map(v => {
    if (wkVocab[v.slug] !== undefined) {
      v.level = wkVocab[v.slug].data.level;
      v.present_in_wk = true;
      return v;
    }
    
    let level = 0;
    for (const k of v.slug.matchAll(RE_KANJI)) {
      if (kanjiByWaniKaniLevel[k] !== undefined) {
        level = Math.max(level, kanjiByWaniKaniLevel[k]);
      } else {
        level = maxLevel+1;
        break;
      }
    }
    
    v.level = level;
    v.present_in_wk = false;
    v.contains_katakana = v.slug.match(RE_KATA) !== null;
    return v;
  });

  vocab.sort((a, b) => {
    if (a.present_in_wk && !b.present_in_wk) {
      return 1;
    } else if (!a.present_in_wk && b.present_in_wk) {
      return -1;
    }
    
    if (a.present_in_wk && b.present_in_wk) {
      return b.level - a.level;
    }
    if (!a.present_in_wk && !b.present_in_wk) {
      return a.level - b.level;
    }
    return 0;
  });
  
  vocab.forEach(v => {
    console.log(`${v.slug},${v.level.toString()}`);
  });
}


// TODO: implement
async function radicalsSync(file) {
  
}


async function radicalsExport() {
  const sm = await axios.get('https://api.wanikani.com/v2/study_materials', {
    headers: HEADERS,
    params: {
      subject_types: 'radical'
    }
  });
  smBySubjectID = {};
  for (const d of sm.data.data) {
    smBySubjectID[d.data.subject_id] = {
      updated_at: d.data_updated_at,
      created_at: d.data.created_at,
      meaning_note: d.data.meaning_note,
      reading_note: d.data.reading_note,
      meaning_synonyms: d.data.meaning_synonyms,
    };
  }

  const kanji = await getSubjects({ types: 'kanji' });
  const kanjiBySubjectID = {};
  const kanjiByChar = {};
  for (const d of kanji) {
    kanjiBySubjectID[d.id] = d;
    kanjiByChar[d.data.characters] = d;
  }

  const radicals = await getSubjects({ types: 'radical' });
  const data = radicals.map(d => {
    const out = {
      radical_subject: {
        id: d.id,
        level: d.data.level,
        slug: d.data.slug,
        document_url: d.data.document_url,
        characters: d.data.characters,
        meanings: d.data.meanings,
        auxiliary_meanings: d.data.auxiliary_meanings,
        amalgamation_subject_ids: d.data.amalgamation_subject_ids.map((sid) => kanjiBySubjectID[sid].data.characters).join(''),
        meaning_mnemonic: d.data.meaning_mnemonic,
      },
      study_material: smBySubjectID[d.id] || {
        updated_at: null,
        created_at: null,
        meaning_note: '',
        reading_note: '',
        meaning_synonyms: [],
      },
    };
    const kanji = kanjiByChar[d.data.characters]
    if (kanji !== undefined) {
      out.kanji_subject = {
        id: kanji.id,
        level: kanji.data.level,
        slug: kanji.data.slug,
        document_url: kanji.data.document_url,
        meanings: kanji.data.meanings,
        auxiliary_meanings: kanji.data.auxiliary_meanings,
        readings: kanji.data.readings,
        component_subject_ids: kanji.data.component_subject_ids,
        visually_similar: kanji.data.visually_similar_subject_ids.map((sid) => kanjiBySubjectID[sid].data.characters).join(''),
        meaning_mnemonic: kanji.data.meaning_mnemonic,
        meaning_hint: kanji.data.meaning_hint,
        reading_mnemonic: kanji.data.reading_mnemonic,
        reading_hint: kanji.data.reading_hint,
      };
    }
    return out;
  });
  console.log(YAML.stringify(data));
  process.exit(0);
}


async function confirm(message) {
  const result = await inquirer.prompt([{
    type: 'confirm',
    message,
    name: 'answer',
  }]);
  return result.answer;
}


// TODO: filter by reading
// TODO: filter by meaning
// TODO: when provided no filter, switch to multi select
async function select(argv) {
  const assignments = await getAssignments({
    unlocked: true,
    immediately_available_for_lessons: true,
  });

  const lessonSubjectIds = {};
  assignments.forEach(a => {
    lessonSubjectIds[a.data.subject_id] = a;
  });

  const subjects = await getSubjects({types: 'vocabulary,kana_vocabulary'});
  const vocabToReview = subjects.filter(subject => {
    if (lessonSubjectIds[subject.id] === undefined) return false;
    
    return subject.data.characters.includes(argv.char_query);
  });

  for (const subject of vocabToReview) {
    console.log(subject.data);
    if (await confirm('Add to reviews?')) {
      await startAssignment(lessonSubjectIds[subject.id].id);
    }
  }
}


const commands = [
  {
    command: 'move',
    description: 'Move JLPT vocabulary to review',
    builder: {
      number: {
        alias: 'n',
        describe: 'Number of items to move to review',
        type: 'number',
        default: 100,
        demandOption: true
      },
      jlpt: {
        alias: 'j',
        describe: 'Number of items to move to review',
        type: 'number',
        default: 3,
      }
    },
    handler: async (argv) => {
      const numItems = argv.number;
      if (numItems !== null && numItems <= 0) {
        console.error('Number of items must be positive');
        process.exit(1);
      }
      await moveJLPTVocabToReview(numItems, argv.jlpt);
      process.exit(0);
    }
  },
  {
    command: 'show',
    description: 'Show comprehensible vocabulary',
    handler: showComprehensibleVocab
  },
  {
    command: 'vocab-list',
    description: 'Show custom vocabulary list',
    handler: customVocabList
  },
  {
    command: 'radicals-sync',
    description: 'Import user-specific study materials for radicals from a YAML file',
    builder: {
      file: {
        alias: 'f',
        describe: 'The file from which to import the radical study materials',
        type: 'file',
        demandOption: true,
      }
    },
    handler: async (argv) => {
      await radicalsImport(argv.file);
      process.exit(0);
    },
  },
  {
    command: 'radicals-export',
    description: 'Export user-specific study materials for radicals',
    handler: radicalsExport
  },
  {
    command: 'select',
    description: 'Search for items in review matching a query, and optionally moves them to review',
    builder: {
      char_query: {
        alias: 'c',
        describe: 'Searches characters for this',
        type: 'string',
        demandOption: false,
      },
      meaning_query: {
        alias: 'm',
        describe: 'Searches meanings for this',
        type: 'string',
        demandOption: false,
      },
      reading_query: {
        alias: 'r',
        describe: 'Searches reading for this',
        type: 'string',
        demandOption: false,
      }
    },
    handler: async (argv) => {
      await select(argv);
      process.exit(0);
    }
  }
];


if (require.main === module) {
  (yargs(hideBin(process.argv))
    .scriptName('wanikani')
    .usage('$0 <cmd> [args]')
    .command(commands)
    .demandCommand(1, 'You need to specify a command')
    .strict()
    .help()
    .alias('help', 'h')
    .version('1.0.0')
    .parse());
}

module.exports = {
  getSubjects,
  getAssignments,
};