//W930S3RS JOB (640W9300100W930S3RS,W100),'RTN W930S3',                         
//         USER=?,PASSWORD=?,                                                   
//         CLASS=K                                                              
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=99,FORMS=1800,LINECT=0                                          
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W930S3                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='WF10.WF10R2.WF1023',                                           
//           T1='WF10.W930S3.WF1023',RF1=FB,LR1=51                              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W930S3RS                                         
