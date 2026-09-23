//W200V1RS JOB (650W0010300W200V1RS,W100),'RTN W200V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//BLOCK  EXEC WBLOCK,NAME=W200V1                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W440.W440D3.W44081',                                           
//           T1='W440.W200V1.W44081',RF1=FB,LR1=11,                             
//*                                                                             
//           F2='W222.W222D1.W22217',                                           
//           T2='W222.W200V1.W22217',RF2=FB,LR2=11,                             
//*                                                                             
//           F3='W222.W222D1.W22219',                                           
//           T3='W222.W200V1.W22219',RF3=FB,LR3=11,                             
//*                                                                             
//           F4='W222.W222D1.W22220',                                           
//           T4='W222.W200V1.W22220',RF4=FB,LR4=11,                             
//*                                                                             
//           F5='W222.W222V1.W22227',                                           
//           T5='W222.W200V1.W22227',RF5=FB,LR5=11,                             
//*                                                                             
//           F6='W217.W200D1.W21733',                                           
//           T6='W217.W200V1.W21733',RF6=FB,LR6=11,                             
//*                                                                             
//           F7='W213.W200D1.W21315',                                           
//           T7='W213.W200V1.W21315',RF7=FB,LR7=11,                             
//*                                                                             
//           F8='WIN.W22104',                                                   
//           T8='W221.W200V1.W22104',RF8=FB,LR8=11,                             
//*                                                                             
//           F9='W222.DUMMY.W22201',                                            
//           T9='W222.W200V1.W22201',RF9=FB,LR9=10                              
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME02 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W222.DUMMY.W22202',                                            
//           T1='W222.W200V1.W22202',RF1=FB,LR1=18,                             
//*                                                                             
//           F2='W222.DUMMY.W22203',                                            
//           T2='W222.W200V1.W22203',RF2=FB,LR2=28,                             
//*                                                                             
//           F3='W222.DUMMY.W22215',                                            
//           T3='W222.W200V1.W22215',RF3=FB,LR3=28                              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W200V1RS                                         
