//W231V1RS JOB (640W2310100W231V1RS,W100),'RTN W231V1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINECT=0,FORMS=1800                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WBLOCK EXEC WBLOCK,NAME=W231V1                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W231.W231D1.W23179',                                           
//           T1='W231.W231V1.W23179',RF1=FB,LR1=32                              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W231V1RS                                         
/*                                                                              
