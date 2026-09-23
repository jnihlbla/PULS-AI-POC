//W200D1RS JOB (650W0010300W200D1RS,W100),'RTN W200D1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W200D1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W092.W092D2.W09222',                                           
//           T1='W092.W200D1.W09222',RF1=FB,LR1=054,                            
//*                                                                             
//           F2='W092.W092D2.W09251',                                           
//           T2='W092.W200D1.W09251',RF2=FB,LR2=022,                            
//*                                                                             
//           F3='W092.W092D2.W09253',                                           
//           T3='W092.W200D1.W09253',RF3=VB,LR3=036,                            
//*                                                                             
//           F4='W092.W092D2.W09280',                                           
//           T4='W092.W200D1.W09280',RF4=VB,LR4=54,                             
//*                                                                             
//           F5='W092.W092D5.W0924A',                                           
//           T5='W092.W200D1.W0924A',RF5=FB,LR5=022,                            
//*                                                                             
//           F6='W092.W092D5.W0924B',                                           
//           T6='W092.W200D1.W0924B',RF6=VB,LR6=32,                             
//*                                                                             
//           F7='W092.W092D5.W0924C',                                           
//           T7='W092.W200D1.W0924C',RF7=FB,LR7=054,                            
//*                                                                             
//           F8='W213.DUMMY.W21331',                                            
//           T8='W213.W200D1.W21331',RF8=FB,LR8=022,                            
//*                                                                             
//           F9='W213.DUMMY.W21336',                                            
//           T9='W213.W200D1.W21336',RF9=FB,LR9=14                              
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME02 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W217.DUMMY.W21701',                                            
//           T1='W217.W200D1.W21701',RF1=VB,LR1=54                              
//SOP     EXEC WSOPEND,PROCESS=W200D1RS                                         
