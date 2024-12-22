#!/usr/bin/env node
const fs = require('fs').promises;

const axios = require('axios');
const yargs = require('yargs');
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


/**
 * Fetches a list of subjects from WaniKani. See here: https://docs.api.wanikani.com/20170710/#get-all-subjects
 * @param {string} types 
 * @returns An array of Subject objects
 */
async function getSubjects(types) {
  var response = await axios.get('https://api.wanikani.com/v2/subjects', {
    headers: HEADERS,
    params: {
      types
    }
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
 * @returns 
 */
async function getAssignments(params) {
  var response = await axios.get('https://api.wanikani.com/v2/assignments', {
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


async function getJLPTVocab() {
  const N5vocab = JSON.parse(await fs.readFile('jlpt-n5.json', 'utf8'));
  const N4vocab = JSON.parse(await fs.readFile('jlpt-n4.json', 'utf8'));

  const RE_DISAMBIG = /-\d$/;
  const vocab = N5vocab.concat(N4vocab);
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
async function moveJLPTVocabToReview(numItems) {
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

      const subjects = await getSubjects('vocabulary,kana_vocabulary');
      
      const N5toN4Words = new Set((await getJLPTVocab()).map(v => v.slug));

      const vocabToReview = subjects.filter(subject => {
        // Skip if already reviewed
        if (lessonSubjectIds[subject.id] === undefined) return false;

        if (subject.data.type === 'kana_vocabulary') return true;
        
        var chars = subject.data.characters;
        if (chars.startsWith('〜')) {
          chars = chars.substring(1);
        }
        if (!N5toN4Words.has(chars)) return false;

        return true;
      }).map(subject => ({
        assignment: lessonSubjectIds[subject.id],
        subject: subject,
      }));

      // Move each vocab to review
      const numToMove = Math.min(vocabToReview.length, numItems);
      for (let i=0; i < numToMove; i++) {
        const vocab = vocabToReview[i];
        const assignment = await axios.put(`https://api.wanikani.com/v2/assignments/${vocab.assignment.id}/start`,
          {assignment: {}},
          {
            headers: {
              'Authorization': `Bearer ${WANIKANI_API_KEY}`,
              'Wanikani-Revision': '20170710',
              'Content-Type': 'application/json'
            }
          }
        );
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
  const assignments = await getAssignments({});
  
  const lessonSubjectIds = {};
  assignments.forEach(a => {
    lessonSubjectIds[a.data.subject_id] = a;
  });
  
  const learnedKanji = new Set((await getSubjects('kanji')).filter(s => {
    return lessonSubjectIds[s.id] !== undefined && lessonSubjectIds[s.id].started_at !== null;
  }).map(s => {
    return s.data.characters;
  }));

  const learnableVocab = new Set((await getSubjects('vocabulary,kana_vocabulary')).map(s => {
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
  var maxLevel = 0;
  for (const k in kanji) {
    kanjiByWaniKaniLevel[k] = kanji[k].wk_level;
    maxLevel = Math.max(maxLevel, kanji[k].wk_level);
  }

  const subjects = await getSubjects('vocabulary,kana_vocabulary');
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
    
    var level = 0;
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
    //console.log(v);
    console.log(`${v.slug},${v.level.toString()}`);
  });
}

/**
 * Obtains vocabulary which can be conjugated.
 * @param {Number} minLevel the minimum SRS stage the vocabulary item should be at to be included.
 */
async function conjugationPractice(minLevel) {
  const assignments = await getAssignments({
    started: true,
    subject_types: 'vocabulary',
  });

  const lessonSubjectIds = {};
  assignments.forEach(a => {
    lessonSubjectIds[a.data.subject_id] = a;
  });

  const subjects = await getSubjects('vocabulary');

  const vocabToReview = subjects.filter(subject => {
    const assignment = lessonSubjectIds[subject.id];
    
    if (assignment === undefined || assignment.data.srs_stage < minLevel) {
      return false;
    }

    for (const pos of subject.data.parts_of_speech) {
      if (['な adjective', 'い adjective', 'intransitive verb', 'ichidan verb', 'transitive verb', 'godan verb', 'verbal noun'].indexOf(pos) !== -1) {
        return true;
      }
    }

    return false;
  }).map(subject => subject.data);
  return vocabToReview;
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
      }
    },
    handler: async (argv) => {
      const numItems = argv.number;
      if (numItems !== null && numItems <= 0) {
        console.error('Number of items must be positive');
        process.exit(1);
      }
      await moveJLPTVocabToReview(numItems);
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
    command: 'conjugate',
    description: 'Start conjugation practice',
    builder: {
      stage: {
        alias: 's',
        describe: 'Minimum SRS stage',
        type: 'number',
        default: null,
        demandOption: true
      }
    },
    handler: async (argv) => {
      const stage = argv.stage;
      if (stage !== null && stage <= 0) {
        console.error('Stage must be positive');
        process.exit(1);
      }
      await conjugationPractice(stage);
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