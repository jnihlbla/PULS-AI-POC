//W412D1RS JOB (640W4120100W412D1RS,W100),'RTN W412D1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W412D1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W412.W412D1.DUMMY',                                            
//           T1='W412.W412D1.W41203',RF1=VB,LR1=164,                            
//*                                                                             
//           F2='D886.D88625',                                                  
//           T2='WIN.W412D1.D88625',RF2=FB,LR2=80,                              
//*                                                                             
//           F3='W418.W418D2.W41836',                                           
//           T3='W418.W412D1.W41836',RF3=FB,LR3=75,                             
//*                                                                             
//           F4='WIN.R234V1.R23436',                                            
//           T4='W412.W412D1.R23436',RF4=FB,LR4=80                              
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME02 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W412.W412X4SE.W41222',                                         
//           T1='W412.W412D1.W41222',RF1=VB,LR1=142,                            
//*                                                                             
//           F2='W412.W412X4PP.W41222',                                         
//           T2='W412.W412D1.W41223',RF2=VB,LR2=142,                            
//*                                                                             
//           F3='W412.W412D1.DUMMY',                                            
//           T3='W412.W412D1.W41208',RF3=VB,LR3=96                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412D1RS                                         
