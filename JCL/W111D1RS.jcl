//W111D1RS JOB (650W1110100W111D1RS,W100),'RTN W111D1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM ,FORMS=1800,LINECT=0                                                  
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W111D1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W513.W510D2.W51385',                                           
//           T1='W513.W111D1.W51385',RF1=FB,LR1=7                               
//SOP     EXEC WSOPEND,PROCESS=W111D1RS                                         
