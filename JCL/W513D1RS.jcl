//W513D1RS JOB (650W5130100W513D1RS,W100),'RTN W513D1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W513D1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W111.W111D1.W11121',                                           
//           T1='W111.W513D1.W11121',RF1=FB,LR1=09,                             
//*                                                                             
//           F2='W116.W116D2.W11638',                                           
//           T2='W116.W513D1.W11638',RF2=FB,LR2=09,                             
//*                                                                             
//           F3='W116.W116D4.W11668',                                           
//           T3='W116.W513D1.W11668',RF3=FB,LR3=09,                             
//*                                                                             
//           F4='W513.W510D2.W51330',                                           
//           T4='W513.W513D1.W51330',RF4=FB,LR4=74,                             
//*                                                                             
//           F5='W513.W510D2.W51331',                                           
//           T5='W513.W513D1.W51331',RF5=FB,LR5=55,                             
//*                                                                             
//           F6='W513.W510D2.W51332',                                           
//           T6='W513.W513D1.W51332',RF6=FB,LR6=74,                             
//*                                                                             
//           F7='W513.W510D2.W51333',                                           
//           T7='W513.W513D1.W51333',RF7=FB,LR7=55,                             
//*                                                                             
//           F8='W116.W116D4.W1167C',                                           
//           T8='W116.W513D1.W1167C',RF8=FB,LR8=09,                             
//*                                                                             
//           F9='W116.W116D2.W1167C',                                           
//           T9='W116.W513D1.W1167C2',RF9=FB,LR9=09                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W513D1RS                                         
