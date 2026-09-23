//W371P2RS JOB (650W3710100W371P2RS,W100),'RTN W371P2',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W371P2                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W371.W371P2.P.W3714A',                                         
//           T1='W371.W371P2.W3714A',RF1=FB,LR1=22,                             
//*                                                                             
//           F2='W371.W371P2.P.W3714B',                                         
//           T2='W371.W371P2.W3714B',RF2=FB,LR2=11,                             
//*                                                                             
//           F3='W371.W371P2.P.W3714C',                                         
//           T3='W371.W371P2.W3714C',RF3=FB,LR3=10,                             
//*                                                                             
//           F4='W371.W371P2.P.W3714D',                                         
//           T4='W371.W371P2.W3714D',RF4=FB,LR4=10,                             
//*                                                                             
//           F5='W371.W371P2.P.W3714E',                                         
//           T5='W371.W371P2.W3714E',RF5=FB,LR5=11                              
//SOP     EXEC WSOPEND,PROCESS=W371P2RS                                         
