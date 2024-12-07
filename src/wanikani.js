const fs = require('fs').promises;

const axios = require('axios');

async function moveN3N5VocabToReview() {
    const WANIKANI_API_KEY = process.env.WANIKANI_API_KEY;
    
    if (!WANIKANI_API_KEY) {
      console.error('Please set WANIKANI_API_KEY in your .env file');
      return;
    }
  
    try {
      // Fetch all subjects
      const assignmentsResponse = await axios.get('https://api.wanikani.com/v2/assignments', {
        headers: {
          'Authorization': `Bearer ${WANIKANI_API_KEY}`,
          'Wanikani-Revision': '20170710'
        },
        params: {
            unlocked: true,
            immediately_available_for_lessons: true,
        }
      });
  
      const lessonSubjectIds = {};
      assignmentsResponse.data.data.forEach(a => {
        lessonSubjectIds[a.data.subject_id] = a;
      });

      const subjectsResponse = await axios.get('https://api.wanikani.com/v2/subjects', {
        headers: {
          'Authorization': `Bearer ${WANIKANI_API_KEY}`,
          'Wanikani-Revision': '20170710'
        },
        params: {
          types: 'vocabulary'
        }
      });

      const N5vocab = JSON.parse(await fs.readFile('jlpt-n5.json', 'utf8'));
      const N4vocab = JSON.parse(await fs.readFile('jlpt-n4.json', 'utf8'));

      const N5toN4Words = new Set(N5vocab.map((e) => e.slug).concat(N4vocab.map((e) => e.slug)));

      const vocabToReview = subjectsResponse.data.data.filter(subject => {
        // Skip if already reviewed
        if (lessonSubjectIds[subject.id] === undefined) return false;
        
        if (!N5toN4Words.has(subject.data.characters)) return false;

        return true;
      }).map(subject => ({
        assignment: lessonSubjectIds[subject.id],
        subject: subject,
      }));

      // Move each vocab to review
      for (const vocab of vocabToReview) {
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
      }
  
      console.log(`Total vocabulary moved to review: ${vocabToReview.length}`);
  
    } catch (error) {
      console.error('Error moving vocabulary to review:', 
        error.response ? error.response.data : error.message
      );
      console.log(error.response.config);
    }
  }
  
// Run the script
moveN3N5VocabToReview();