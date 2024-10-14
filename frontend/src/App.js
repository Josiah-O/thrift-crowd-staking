import React from 'react';
import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import { Toaster } from 'react-hot-toast';
import Header from './components/Header';
import Home from './components/Home';
import CreateCSG from './components/CreateCSG';
import CSGDetails from './components/CSGDetails';
import JoinCSG from './components/JoinCSG';
import MyCSGs from './components/MyCSGs';
import { WalletProvider } from './contexts/WalletContext';

function App() {
  return (
    <WalletProvider>
      <Router>
        <div className="App">
          <Header />
          <main className="container mx-auto px-4 py-8">
            <Routes>
              <Route path="/" element={<Home />} />
              <Route path="/create-csg" element={<CreateCSG />} />
              <Route path="/csg/:csgId" element={<CSGDetails />} />
              <Route path="/join-csg/:csgId" element={<JoinCSG />} />
              <Route path="/my-csgs" element={<MyCSGs />} />
            </Routes>
          </main>
          <Toaster position="top-right" />
        </div>
      </Router>
    </WalletProvider>
  );
}

export default App;
