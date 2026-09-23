//W092D2RS JOB (650W0920100W092D2RS,W100),'RTN W092D2',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W092D2                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W212.W212S1.EPICSE',                                           
//           T1='W212.W092D2.EPICSE',RF1=VB,LR1=86,                             
//*                                                                             
//           F2='W092.W092X5PP.NAPIN',                                          
//           T2='W092.W092D2.NAPIN',RF2=FB,LR2=427,                             
//*                                                                             
//           F3='W092.W09204',                                                  
//           T3='W092.W092D2.W09204',RF3=VB,LR3=104                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W092D2RS                                         
