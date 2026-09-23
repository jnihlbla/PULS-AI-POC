//WF10R2RS JOB (640WF100100WF10R2RS,W100),'RTN WF10R2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=WF10R2                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME03 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='WF10.WF10R1.WF1021',                                           
//           T1='WF10.WF10R2.WF1021',RF1=FB,LR1=0046,                           
//*                                                                             
//           F2='WF10.WF10R3.WF1022',                                           
//           T2='WF10.WF10R2.WF1022',RF2=FB,LR2=0046                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10R2RS                                         
