//W100V2RS JOB (650W0010300W100V2RS,W100),'RTN W100V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W100V2                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W092.W092D6.W09291',                                           
//           T1='W092.W100V2.W09291',RF1=FB,LR1=093,                            
//*                                                                             
//           F2='W092.W092D6.W092ZV',                                           
//           T2='W092.W100V2.W092ZV',RF2=FB,LR2=040,                            
//*                                                                             
//           F3='W092.W092D6.W092Z5',                                           
//           T3='W092.W100V2.W092Z5',RF3=FB,LR3=016                             
//SOP     EXEC WSOPEND,PROCESS=W100V2RS                                         
