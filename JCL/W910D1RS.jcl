//W910D1RS JOB (640W9100100W910D1RS,W100),'RTN W910D1',                         
//         USER=?,PASSWORD=?,                                                   
//         CLASS=K                                                              
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=99,FORMS=1800,LINECT=0                                          
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK    EXEC WBLOCK,NAME=W910D1                                              
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W111.W111D1.W111PP',                                           
//           T1='W111.W910D1.W111PP',RF1=FB,LR1=27,                             
//*                                                                             
//           F2='W111.W111D1.W11115',                                           
//           T2='W111.W910D1.W11115',RF2=FB,LR2=27                              
//SOP     EXEC WSOPEND,PROCESS=W910D1RS                                         
