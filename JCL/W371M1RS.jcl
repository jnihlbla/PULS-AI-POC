//W371M1RS JOB (640W3710100W371M1RS,W100),'RTN W371M1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W371M1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W371.W371D1.W37132',                                           
//           T1='W371.W371M1.W37132',RF1=FB,LR1=132,                            
//*                                                                             
//           F2='W371.W371V1.W371NA',                                           
//           T2='W371.W371M1.W371NA',RF2=FB,LR2=132                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371M1RS                                         
