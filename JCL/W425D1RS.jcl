//W425D1RS JOB (650W4250100W425D1RS,W100),'RTN W425D1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W425D1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W092.W092D6.W092Z7',                                           
//           T1='W092.W425D1.W092Z7',RF1=VB,LR1=118,                            
//*                                                                             
//           F2='W476.W476D5.W47662',                                           
//           T2='W476.W425D1.W47662',RF2=VB,LR2=195,                            
//*                                                                             
//           F3='W440.W440D1.W44026',                                           
//           T3='W440.W425D1.W44026',RF3=VB,LR3=118,                            
//*                                                                             
//           F4='W414.W414D1.W41406',                                           
//           T4='W414.W425D1.W41406',RF4=VB,LR4=118                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W425D1RS                                         
