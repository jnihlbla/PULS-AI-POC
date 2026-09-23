//W115D2RS   JOB (640W1150100W115D2RS,W100),'RTN W115D2',                       
//             CLASS=K,USER=?,PASSWORD=?                                        
/*JOBPARM LINECT=0,FORMS=1800                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WBLOCK EXEC WBLOCK,NAME=W115D2                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W414.W414D1.W4140E',                                           
//           T1='W414.W115D2.W4140E',RF1=FB,LR1=032                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W115D2RS                                         
/*                                                                              
