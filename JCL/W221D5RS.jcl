//W221D5RS JOB (640W2210100W221D5RS,W100),'RTN W221D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W221D5                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W221.W221D4.W22186',                                           
//           T1='W221.W221D5.D4.W22186',RF1=FB,LR1=005,                         
//*                                                                             
//           F2='W221.W221V2.W22186',                                           
//           T2='W221.W221D5.V2.W22186',RF2=FB,LR2=005                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221D5RS                                         
