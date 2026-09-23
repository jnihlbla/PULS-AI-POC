//W331M9RS JOB (650W3310100W331M9RS,W100),'RTN W331M9',                         
//             CLASS=K,USER=? PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W331M9                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='WIN.QASE.W33170',                                              
//           T1='W331.W331M9.W33170',RF1=FB,LR1=383                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W331M9RS                                         
