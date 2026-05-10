open Brr

let style_css = {|
@import url(https://fonts.googleapis.com/css?family=Raleway:300,400,500,600,700,800);

:root {
  --primary-color: #ffcd42;
  --secondary-color: #ffd35c;
  --accent-color: #1f7ae0;
  --bg-primary: #ffffff;
  --bg-soft: #f7f7f2;
  --text-color: #191919;
  --text-color-two: #ffffff;
  --bg-secondary: #111111;
  --card-background: #efefea;
  --bg-secondary-two: #202020;
  --border-color: #e4e4df;
  --shadow: 0 12px 34px rgba(0, 0, 0, 0.14);

  --weight-small: 400;
  --weight-semibold: 600;
  --weight-bold: 800;

  --width-small: 600px;
  --width-medium: 1100px;
  --width-large: 1300px;
}

[data-theme="dark"] {
  --primary-color: #ffcd42;
  --secondary-color: #ffd35c;
  --accent-color: #6cb0ff;
  --bg-primary: #0d0d0d;
  --bg-soft: #151515;
  --text-color: #f1f1f1;
  --text-color-two: #111111;
  --bg-secondary: #f3f3f3;
  --card-background: #1a1a1a;
  --bg-secondary-two: #ececec;
  --border-color: #2f2f2f;
  --shadow: 0 8px 20px rgba(0, 0, 0, 0.4);
}

html {
  font-size: 100%;
  scroll-behavior: smooth;
  background: var(--bg-primary);
}

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

ul {
  list-style: none;
}

a {
  text-decoration: none;
  color: var(--text-color);
}

body {
  background: transparent;
  color: var(--text-color);
  font-family: "Raleway", sans-serif;
  line-height: 1.55;
  position: relative;
}

#bg-canvas {
  z-index: 0;
}

#hero {
  position: relative;
}

.header-container {
  position: relative;
  z-index: 1;
}

#projects,
.social-proof,
#footer {
  position: relative;
  z-index: 1;
}

.navbar {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  z-index: 20;
  background: var(--bg-primary);
  border-bottom: 1px solid transparent;
  transition: box-shadow 0.3s ease-in-out, border-color 0.3s ease-in-out;
}

.navbar.scrolled {
  box-shadow: var(--shadow);
  border-color: var(--border-color);
}

.navbar .container {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 2rem;
  padding-right: 2rem;
  min-height: 40px;
  width: 100%;
}

.navbar #logo img {
  display: block;
  width: min(160px, 48vw);
}

.navbar .nav-menu {
  display: flex;
  align-items: center;
  gap: 0.55rem;
  background: transparent;
}

.navbar .nav-link {
  font-size: 0.78rem;
  font-weight: var(--weight-semibold);
}

.theme-switch-wrap {
  margin-left: 0.35rem;
}

.fas.fa-arrow-right {
  margin-left: 0.45rem;
  font-size: 0.82rem;
}

.hamburger {
  display: none;
  border: 0;
  background: transparent;
}

.bar {
  display: block;
  width: 23px;
  height: 3px;
  margin: 4px auto;
  transition: all 0.3s ease-in-out;
  border-radius: 30px;
  background-color: var(--bg-secondary);
}

#hero {
  display: flex;
  flex-direction: column;
  padding-top: 160px;
}

.header-container {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: flex-start;
  margin-top: 0;
  padding: 1.5rem 1rem 1rem;
}

.eyebrow {
  font-size: 0.8rem;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: color-mix(in srgb, var(--text-color) 66%, transparent);
}

.header-container>h1 {
  margin-top: 0.3rem;
  font-size: clamp(2rem, 6vw, 3rem);
}

.content-text {
  text-align: center;
  margin: 1.4rem 0;
}

.content-text h2 {
  font-size: clamp(1.8rem, 5vw, 2.7rem);
  line-height: 1.15;
}

.content-text p {
  margin: 0 auto;
  max-width: 730px;
  padding: 0.55rem;
}

.hero-cta-row {
  display: flex;
  gap: 0.7rem;
  flex-wrap: wrap;
  justify-content: center;
}

.impact-pills {
  margin-top: 1.4rem;
  display: flex;
  gap: 0.55rem;
  flex-wrap: wrap;
  justify-content: center;
}

.impact-pills li {
  border: 1px solid var(--border-color);
  background: var(--bg-soft);
  border-radius: 999px;
  padding: 0.4rem 0.8rem;
  font-size: 0.77rem;
}

.division {
  width: 100%;
  height: 1px;
  background-color: var(--border-color);
  margin: 2rem 0 2rem;
}

#projects {
  display: flex;
  flex-direction: column;
  margin: 0 auto 5rem;
}

.project {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  grid-auto-rows: minmax(260px, auto);
  gap: 1rem;
}

.project-card {
  min-height: 280px;
  position: relative;
  overflow: hidden;
}

.project-card::before {
  content: "";
  position: absolute;
  inset: 0;
  background: linear-gradient(to top, rgba(0, 0, 0, 0.82) 8%, rgba(0, 0, 0, 0.16) 68%);
}

.project-info {
  position: relative;
  z-index: 1;
  display: flex;
  height: 100%;
  justify-content: space-between;
  align-items: flex-end;
  gap: 0.75rem;
}

.project-bio {
  display: flex;
  flex-direction: column;
  gap: 0.2rem;
}

.project-bio h3 {
  font-size: 1rem;
}

.project-bio p {
  font-size: 0.83rem;
}

.project-status {
  display: inline-flex;
  align-items: center;
  width: fit-content;
  margin-top: 0.35rem;
  border-radius: 999px;
  padding: 0.18rem 0.55rem;
  background: color-mix(in srgb, var(--primary-color) 78%, #ffffff 22%);
  color: #111;
  font-weight: var(--weight-semibold);
  font-size: 0.72rem;
}

.project-link {
  margin-bottom: 0.2rem;
}

.card.project-card--nucleoid {
  background: url(./assets/project/NAP.png) center center/cover;
}

.card.project-card--tubulin {
  background: url(./assets/project/tubulin.gif) center center/cover;
}

.card.project-card--actin {
  background: url(./assets/project/MPActs.gif) center center/cover;
}

.card.project-card--qgp {
  background: url(./assets/project/QGP.gif) center center/cover;
}

.card.project-card--slurmgen {
  background: url(./assets/project/slurmgen.jpg) center center/cover;
}

.social-proof {
  margin-bottom: 4rem;
}

.social-icons {
  display: flex;
  justify-content: center;
  gap: 0.9rem;
  flex-wrap: wrap;
}

.social-icons img {
  width: 26px;
  height: 26px;
  opacity: 0.9;
}

.social-icons i {
  font-size: 1.1rem;
  color: var(--text-color);
}

.social-icons a {
  display: inline-flex;
  border: 1px solid var(--border-color);
  background: var(--bg-soft);
  border-radius: 14px;
  padding: 0.6rem;
  transition: transform 0.2s ease, border-color 0.2s ease;
}

.social-icons a:hover {
  transform: translateY(-3px);
  border-color: color-mix(in srgb, var(--primary-color) 70%, var(--border-color));
}

#footer {
  background: #111111;
}

#footer .container {
  display: flex;
  align-items: center;
  justify-content: center;
  color: #ffffff;
  min-height: 140px;
  text-align: center;
}

#footer a {
  font-size: 0.8rem;
  color: #fff;
}

#footer a:hover {
  opacity: 0.7;
}

@media (max-width: 1000px) {
  .project {
    grid-template-columns: 1fr;
  }
}

@media (max-width: 670px) {
  .navbar .container {
    padding-left: 1rem;
    padding-right: 1rem;
  }

  .navbar .nav-menu {
    position: fixed;
    right: -100vw;
    top: 4.6rem;
    flex-direction: column;
    width: min(340px, calc(100% - 30px));
    transition: right 0.3s ease;
    box-shadow: var(--shadow);
    padding: 1rem;
    border-radius: 9px;
    align-items: stretch;
    background: var(--bg-primary);
    border: 1px solid var(--border-color);
  }

  .nav-menu.active {
    right: 15px;
  }

  .nav-menu li {
    width: 100%;
  }

  .nav-menu .btn {
    width: 100%;
    text-align: center;
  }

  .theme-switch-wrap {
    display: flex;
    justify-content: center;
    margin-top: 0.3rem;
  }

  .hamburger {
    display: block;
    cursor: pointer;
    padding: 0.2rem 0.4rem;
  }

  .hamburger.active .bar:nth-child(2) {
    opacity: 0;
  }

  .hamburger.active .bar:nth-child(1) {
    transform: translateY(7px) rotate(45deg);
  }

  .hamburger.active .bar:nth-child(3) {
    transform: translateY(-7px) rotate(-45deg);
  }
}

@media (max-width: 600px) {

  .hero-cta-row,
  .hero-cta-row .btn {
    width: 100%;
  }

  .hero-cta-row .btn {
    border-radius: 8px;
  }

  .impact-pills {
    justify-content: flex-start;
  }

  .impact-pills li {
    width: 100%;
  }
}
|}

let utilities_css = {|
.container {
  max-width: var(--width-medium);
  margin: 0 auto;
  padding: 1rem 2rem;
}

h1::selection,
h2::selection {
  color: #111;
  background: var(--primary-color);
}

.btn {
  display: inline-block;
  padding: 0.75rem 1.15rem;
  border-radius: 999px;
  text-transform: uppercase;
  letter-spacing: 0.03em;
  font-size: 0.78rem;
  font-weight: var(--weight-semibold);
  transition: 0.25s ease;
  border: 1px solid transparent;
}

.btn-primary {
  background: var(--primary-color);
  color: #000000;
}

.btn-primary:hover {
  background: var(--secondary-color);
}

.btn-secondary {
  background-color: var(--bg-secondary);
  color: var(--bg-primary);
}

.btn-secondary:hover {
  opacity: 0.9;
}

.btn-outline {
  border-color: var(--border-color);
  background: transparent;
  color: var(--text-color);
}

.btn-outline:hover {
  border-color: color-mix(in srgb, var(--primary-color) 75%, var(--border-color));
  background: var(--bg-soft);
}

#switch {
  display: none;
}

.toggle-icons {
  display: flex;
  justify-content: space-between;
  align-items: center;
  cursor: pointer;
  gap: 0.15rem;
}

.toggle-icons>img {
  transition: 0.5s cubic-bezier(0.23, 1, 0.32, 1);
  width: 28px;
}

.moon {
  transform: rotate(10deg);
}

#switch:checked+.toggle-icons .moon {
  transform: rotate(250deg);
}

#switch:checked+.toggle-icons .sun {
  transform: rotate(100deg);
}

.card {
  padding: 1rem;
  background: transparent;
  color: #ffffff;
  border-radius: 10px;
  transition: 0.3s ease-in-out;
}

.card a {
  color: #ffffff;
  transition: 0.2s ease-in-out;
  font-size: 1.1rem;
}

.card a:hover {
  color: var(--primary-color);
}

.card:hover {
  transform: translateY(-3px);
  box-shadow: var(--shadow);
}

@media (max-width: 768px) {
  .container {
    padding: 0.9rem 1rem;
  }
}
|}

let resume_css = {|
/* Resume specific styles that complement the main style.css */

/* Resume content section */
#resume-content {
    margin-top: 1rem;
}

#resume-content .section {
    margin-bottom: 2rem;
}
/* Resume content text styling */
#resume-content .content-text {
    text-align: left;
    margin-bottom: 2rem;
}

#resume-content .content-text p {
    margin: 5px 0;
    font-size: 1.1em;
    color: var(--text-color);
}
/* Resume specific links */
#resume-content a {
    color: var(--primary-color);
    text-decoration: none;
}
#resume-content a:hover {
    text-decoration: underline;
}

/* Custom header styling for resume page */
.header-container {
    margin-top: 4rem;
}

.header-container h1 {
    font-size: 2.2rem;
}

.header-container h2 {
    font-size: 1.6rem;
    margin-top: 0.5rem;
}
.section {
    margin-bottom: 0.5rem;
}
.section h2 {
    padding-bottom: 0.5rem;
    margin-bottom: 1rem;
    font-size: 1.8em;
    color: var(--text-color);
    text-align: left;
}
.job, .education-item {
    margin-bottom: 1rem;
}
.job-title, .degree {
    font-weight: var(--weight-semibold);
    font-size: 1.2em;
    color: var(--text-color);
}
.company, .school {
    font-style: italic;
    color: var(--text-color);
    opacity: 0.8;
}
.date {
    float: right;
    color: var(--text-color);
    opacity: 0.7;
}
ul.highlights {
    list-style-type: disc;
    padding-left: 20px;
    color: var(--text-color);
}
.skills-section ul {
    list-style-type: none;
    padding: 0;
}
.skills-section li {
    padding: 8px 0;
    color: var(--text-color);
}
.skills-category {
    font-weight: var(--weight-semibold);
    color: var(--primary-color);
}

/* Media queries for responsive resume */
@media (max-width: 768px) {
    #resume-content {
        margin-top: 1rem;
    }
    
    .section h2 {
        font-size: 1.5em;
    }
    
    .date {
        float: none;
        display: block;
        margin-bottom: 5px;
    }
    
    .header-container h1 {
        font-size: 1.8rem;
    }
    
    .header-container h2 {
        font-size: 1.4rem;
    }
}
|}

let inject css_text =
  let style = El.v (Jstr.v "style") [] in
  El.set_prop (El.Prop.jstr (Jstr.v "innerText")) (Jstr.v css_text) style;
  let head = Document.head G.document in
  El.set_children head (style :: (El.children head))

let inject_main () =
  inject style_css;
  inject utilities_css

let inject_resume () =
  inject_main ();
  inject resume_css
