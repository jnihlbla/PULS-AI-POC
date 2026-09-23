//W261B1RS JOB (650W2610100W261B1RS,W100),'RTN W261B1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ  LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W261B1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W261.W261P1.W26137',                                           
//           T1='W261.W261B1.W26137',RF1=FB,LR1=337,                            
//*                                                                             
//           F2='W261.W261P1.W26138',                                           
//           T2='W261.W261B1.W26138',RF2=FB,LR2=337                             
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=W261B1RS                                         
/*                                                                              
