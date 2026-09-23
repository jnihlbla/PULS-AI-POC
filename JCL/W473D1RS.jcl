//W473D1RS JOB (640W4730100W473D1RS,W100),'RTN W473D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W473D1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W092.W092D6.W092ZW',                                           
//           T1='W092.W473D1.W092ZW',RF1=FB,LR1=25,                             
//*                                                                             
//           F2='W476.W476D1.W47628',                                           
//           T2='W476.W473D1.W47628',RF2=FB,LR2=32                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W473D1RS                                         
