//W225D2RS JOB (640W2250100W225D2RS,W100),'RTN W225D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W225D2                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W092.W092D6.W092S1',                                           
//           T1='W092.W225D2.W092S1',RF1=FB,LR1=141,                            
//*                                                                             
//           F2='W414.W414D1.W4140D',                                           
//           T2='W414.W225D2.W4140D',RF2=FB,LR2=032,                            
//*                                                                             
//           F3='W414.W414D1.W4140I',                                           
//           T3='W414.W225D2.W4140I',RF3=FB,LR3=024,                            
//*                                                                             
//           F4='W412.W412D7.W41241A',                                          
//           T4='W412.W225D2.W41241A',RF4=FB,LR4=058,                           
//*                                                                             
//           F5='W412.W412D7.W41244',                                           
//           T5='W412.W225D2.W41244',RF5=FB,LR5=058                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W225D2RS                                         
