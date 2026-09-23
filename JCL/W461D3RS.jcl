//W461D3RS JOB (640W4610100W461D3RS,W100),'RTN W461D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W461D3                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W461.W461D1.W4612D',                                           
//           T1='W461.W461D3.W4612D',RF1=VB,LR1=215,                            
//*                                                                             
//           F2='W461.W461D1.W4612E',                                           
//           T2='W461.W461D3.W4612E',RF2=VB,LR2=215,                            
//*                                                                             
//           F3='W476.W476D7.W476CA1',                                          
//           T3='W476.W461D3.W476CA1',RF3=VB,LR3=215,                           
//*                                                                             
//           F4='W476.W476D7.W476US2',                                          
//           T4='W476.W461D3.W476US2',RF4=VB,LR4=215                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W461D3RS                                         
