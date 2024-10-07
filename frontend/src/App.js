import React from 'react';
import { BrowserRouter as Router, Route, Routes } from 'react-router-dom';
import { ToastContainer } from 'react-toastify';
import 'react-toastify/dist/ReactToastify.css';
import { WalletProvider } from './contexts/WalletContext';
import Header from './components/Header';
import Footer from './components/Footer';
import Home from './components/Home';
import CreateCSG from './components/CreateCSG';
import CSGDetails from './components/CSGDetails';
import JoinCSG from './components/JoinCSG';
import MyCSGs from './components/MyCSGs';

function App() {
  return (
    <WalletProvider>
      <Router>
        <div className="flex flex-col min-h-screen">
          <Header />
          <main className="flex-grow container mx-auto px-4 py-8">
            <Routes>
              <Route path="/" element={<Home />} />
              <Route path="/create-csg" element={<CreateCSG />} />
              <Route path="/csg/:csgId" element={<CSGDetails />} />
              <Route path="/join-csg/:csgId" element={<JoinCSG />} />
              <Route path="/my-csgs" element={<MyCSGs />} />
            </Routes>
          </main>
          <Footer />
        </div>
      </Router>
      <ToastContainer position="bottom-right" />
    </WalletProvider>
  );
}

export default App;
