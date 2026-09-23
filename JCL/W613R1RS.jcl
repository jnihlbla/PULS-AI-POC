//W613R1RS JOB (640W6130100W613R1RS,W100),'RTN W613R1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST6                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W613R1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W613.W613V2.W61314',                                           
//           T1='W613.W613R1.W61314',RF1=FB,LR1=39,                             
//*                                                                             
//           F2='W613.W613V2.W61318',                                           
//           T2='W613.W613R1.W61318',RF2=FB,LR2=39                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W613R1RS                                         
