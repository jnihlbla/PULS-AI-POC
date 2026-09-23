//W218V1RS JOB (640W2180100W218V1RS,W100),'RTN W218V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W218V1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W222.W222V1.W22221C',                                          
//           T1='W222.W218V1.W22221C',RF1=FB,LR1=5,                             
//*                                                                             
//           F2='W221.W200V1.W22148',                                           
//           T2='W221.W218V1.W22148',RF2=FB,LR2=5,                              
//*                                                                             
//           F3='W221.W200V1.W22139',                                           
//           T3='W221.W218V1.W22139',RF3=FB,LR3=227                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W218V1RS                                         
