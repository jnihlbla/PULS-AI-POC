//WF10D2RS JOB (640WF100100WF10D2RS,W100),'RTN WF10D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=WF10D2                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='WF10.WF10D1.WF1014',                                           
//           T1='WF10.WF10D2.WF1014',RF1=FB,LR1=0044,CP1=10,                    
//*                                                                             
//           F2='WF10.WF10D1.WF1015',                                           
//           T2='WF10.WF10D2.WF1015',RF2=FB,LR2=0034,CP2=10,                    
//*                                                                             
//           F3='WF10.WF10D1.WF1016',                                           
//           T3='WF10.WF10D2.WF1016',RF3=FB,LR3=0036,CP3=10,                    
//*                                                                             
//           F4='WF10.WF10D1.WF1017',                                           
//           T4='WF10.WF10D2.WF1017',RF4=FB,LR4=0087,CP4=10,                    
//*                                                                             
//           F5='WF10.WF10D1.WF1018',                                           
//           T5='WF10.WF10D2.WF1018',RF5=FB,LR5=0479,CP5=10                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10D2RS                                         
