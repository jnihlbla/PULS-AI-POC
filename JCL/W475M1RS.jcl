//W475M1RS JOB (650W4750100W475M1RS,W100),'RTN W475M1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W475M1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W475.W475D2.W4758T',                                           
//           T1='W475.W475M1.W4758T',RF1=FB,LR1=079,                            
//*                                                                             
//           F2='W475.W475D2.W47587',                                           
//           T2='W475.W475M1.W47587',RF2=FB,LR2=079,                            
//*                                                                             
//           F3='W475.W475D2.W47588',                                           
//           T3='W475.W475M1.W47588',RF3=FB,LR3=079,                            
//*                                                                             
//           F4='W510.W510D3.W51046',                                           
//           T4='W510.W475M1.W51046',RF4=FB,LR4=079                             
//SOP     EXEC WSOPEND,PROCESS=W475M1RS                                         
