//W114D2RS   JOB (640W1140100W114D2RS,W100),'RTN W114D2',                       
//             CLASS=K,USER=?,PASSWORD=?                                        
/*JOBPARM LINECT=0,FORMS=1800                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WBLOCK EXEC WBLOCK,NAME=W114D2                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W212.W200D1.W21211',                                           
//           T1='W212.W114D2.W21211',RF1=FB,LR1=054                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W114D2RS                                         
/*                                                                              
