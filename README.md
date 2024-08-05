### 💻 About

This project is a full-stack application built in Haskell, featuring both a frontend and a backend. The backend is a REST API server that manages student data, while the frontend is a web application that interacts with the backend to display and manage student information.

###⚙️ Features

Backend REST API for managing student data
Create, delete, and list students
Data stored in-memory using IORef
Frontend web application
Fetch and display student data from the backend API
Add and delete students
Calculate and display final (harmonic) average and grades
Real-time data updates

### 📷 View
<img src="/assets/images/projetoHaskellCaptura.png"/>

### 🛠️ Installation/Prerequisites

To run this project, you need to have the following tools installed:

Haskell Stack: The build tool for Haskell projects.
Installation guide: Haskell Stack
###📜 Usage

Start the Backend:

Navigate to the backend directory.
Execute stack run.
The server will be ready when you see: Setting phasers to stun... (port 8080) (ctrl-c to quit).
The backend API can be tested using tools like Postman or Curl.
Start the Frontend:

Navigate to the frontend directory.
Execute stack run.
The application will be ready when you see: Spock is running on port 8081.
Access the application at: http://localhost:8081/.
###📆 Future Work

Upcoming contributions (enhancements and fixes) for this project:

Implement more comprehensive validation for input data.
Improve the user interface for better usability.
Add more detailed error handling and logging.
Enhance the data update strategy for even better real-time performance.
