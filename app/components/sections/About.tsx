'use client'

import AnimatedText from "../common/AnimatedText"

export default function About() {
  return (
    <section id="about" className="min-h-screen w-full flex items-center justify-center p-8">
      <div className="max-w-2xl space-y-12">
        <AnimatedText>
          <h1 className="text-4xl font-bold mb-8">About Me</h1>
        </AnimatedText>

        <div className="space-y-8">
          <AnimatedText>
            <div className="space-y-4">
              <p className="text-lg text-foreground/80 leading-relaxed">
                Here, you can introduce your basic background information. Include your name, major, education and other basic information. This is an excellent place to showcase your personal characteristics.
              </p>
              <p className="text-lg text-foreground/80 leading-relaxed">
                You can describe your professional skills and experience in detail. Highlight your tech stack, project experience, and achievements in relevant fields. This information will help visitors better understand your professional capabilities.
              </p>
            </div>
          </AnimatedText>

          <AnimatedText>
            <div className="space-y-4">
              <h2 className="text-2xl font-semibold">Personal Interests</h2>
              <p className="text-lg text-foreground/80 leading-relaxed">
                In this section, you can share your personal interests and hobbies. This content can make your profile more vibrant and showcase your unique personality traits.
              </p>
            </div>
          </AnimatedText>

          <AnimatedText>
            <div className="space-y-4">
              <h2 className="text-2xl font-semibold">Career Goals</h2>
              <p className="text-lg text-foreground/80 leading-relaxed">
                Here you can describe your career development plans and goals. Clearly express the types of opportunities you are looking for and the goals you hope to achieve in your future career.
              </p>
            </div>
          </AnimatedText>
        </div>
      </div>
    </section>
  )
}