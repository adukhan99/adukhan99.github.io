const hamburger = document.querySelector(".hamburger");
const navMenu = document.querySelector(".nav-menu");
const navLinks = document.querySelectorAll(".nav-link");

if (hamburger && navMenu) {
  const closeMenu = () => {
    hamburger.classList.remove("active");
    navMenu.classList.remove("active");
    hamburger.setAttribute("aria-expanded", "false");
  };

  hamburger.addEventListener("click", () => {
    const isActive = hamburger.classList.toggle("active");
    navMenu.classList.toggle("active");
    hamburger.setAttribute("aria-expanded", String(isActive));
  });

  navLinks.forEach((link) => {
    link.addEventListener("click", closeMenu);
  });

  document.addEventListener("keydown", (event) => {
    if (event.key === "Escape") {
      closeMenu();
    }
  });
}

const toggleSwitch = document.querySelector('.theme-switch input[type="checkbox"]');
const root = document.documentElement;

if (toggleSwitch) {
  const currentTheme = localStorage.getItem("theme");

  if (currentTheme) {
    root.setAttribute("data-theme", currentTheme);
    toggleSwitch.checked = currentTheme === "dark";
  }

  toggleSwitch.addEventListener("change", (event) => {
    const theme = event.target.checked ? "dark" : "light";
    root.setAttribute("data-theme", theme);
    localStorage.setItem("theme", theme);
  });
}

const emailLinks = document.querySelectorAll("[data-email-link='true']");
const encodedEmail = "YWdkdWtoYW5AZ21haWwuY29t";

emailLinks.forEach((link) => {
  link.setAttribute("href", `mailto:${atob(encodedEmail)}`);
});

const navbar = document.querySelector(".navbar");
if (navbar) {
  const setNavbarState = () => {
    navbar.classList.toggle("scrolled", window.scrollY > 8);
  };

  setNavbarState();
  window.addEventListener("scroll", setNavbarState, { passive: true });
}

const yearEl = document.querySelector("#datee");
if (yearEl) {
  yearEl.textContent = String(new Date().getFullYear());
}
