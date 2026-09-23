//W461D4RS JOB (640W4610100W461D4RS,W100),'RTN W461D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//BLOCK   EXEC WBLOCK,NAME=W461D4                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W461.W461D1.W46180',                                           
//           T1='W461.W461D4.W46180',RF1=VB,LR1=215,                            
//*                                                                             
//           F2='W461.W461D1.W4612J',                                           
//           T2='W461.W461D4.W4612J',RF2=VB,LR2=215,                            
//*                                                                             
//           F3='W461.W461D1.W4612H',                                           
//           T3='W461.W461D4.W4612H',RF3=VB,LR3=215,                            
//*                                                                             
//           F4='W476.W476D7.W476AU2',                                          
//           T4='W476.W461D4.W476AU2',RF4=VB,LR4=215,                           
//*                                                                             
//           F5='W476.W476D7.W476JP1',                                          
//           T5='W476.W461D4.W476JP1',RF5=VB,LR5=215,                           
//*                                                                             
//           F6='W476.W476D7.W476TH1',                                          
//           T6='W476.W461D4.W476TH1',RF6=VB,LR6=215,                           
//*                                                                             
//           F7='W476.W476D7.W476CN1',                                          
//           T7='W476.W461D4.W476CN1',RF7=VB,LR7=215,                           
//*                                                                             
//           F8='W476.W476D7.W476ZA1',                                          
//           T8='W476.W461D4.W476ZA1',RF8=VB,LR8=215,                           
//*                                                                             
//           F9='W461.W461D1.W461CN2',                                          
//           T9='W461.W461D4.W461CN2',RF9=VB,LR9=215                            
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME02 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W461.W461D1.W461ZA2',                                          
//           T1='W461.W461D4.W461ZA2',RF1=VB,LR1=215,                           
//*                                                                             
//           F2='W476.W476D7.W476CN2',                                          
//           T2='W476.W461D4.W476CN2',RF2=VB,LR2=215,                           
//*                                                                             
//           F3='W461.W461D1.W461CN1',                                          
//           T3='W461.W461D4.W461CN1',RF3=VB,LR3=215,                           
//*                                                                             
//           F4='W461.W461D1.W46126',                                           
//           T4='W461.W461D4.W46126',RF4=VB,LR4=215,                            
//*                                                                             
//           F5='W476.W476D7.W476IN1',                                          
//           T5='W476.W461D4.W476IN1',RF5=VB,LR5=215,                           
//*                                                                             
//           F6='W461.W461D1.W4612F',                                           
//           T6='W461.W461D4.W4612F',RF6=VB,LR6=215,                            
//*                                                                             
//           F7='W476.W476D7.W476KR1',                                          
//           T7='W476.W461D4.W476KR1',RF7=VB,LR7=215,                           
//*                                                                             
//           F8='W461.W461D1.W4612I',                                           
//           T8='W461.W461D4.W4612I',RF8=VB,LR8=215,                            
//*                                                                             
//           F9='W461.W461D1.W4612A',                                           
//           T9='W461.W461D4.W4612A',RF9=VB,LR9=215                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME03 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W476.W476D7.W476TW2',                                          
//           T1='W476.W461D4.W476TW2',RF1=VB,LR1=215,                           
//*                                                                             
//           F2='W476.W476D7.W476MY1',                                          
//           T2='W476.W461D4.W476MY1',RF2=VB,LR2=215                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W461D4RS                                         
