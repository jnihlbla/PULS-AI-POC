//W510D4RS JOB (650W5100100W510D4RS,W100),'RTN W510D4',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W510D4                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W510.W510D2.W51063',                                           
//           T1='W510.W510D4.W51063',RF1=FB,LR1=304,CP1=15,                     
//*                                                                             
//           F2='W510.W510D2.W5103A',                                           
//           T2='W510.W510D4.W5103A',RF2=FB,LR2=096,CP2=15,                     
//*                                                                             
//           F3='W510.W510S1.W5106A',                                           
//           T3='W510.W510D4.W5106A',RF3=FB,LR3=310,CP3=15                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W510D4RS                                         
