//W351V1RS JOB (670W3510100W351V1RS,W100),'RTN W351V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=9,FORMS=1800,LINECT=0                                           
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W351V1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W351.W351X3AT.W35111',                                         
//           T1='W351.W351V1.W35111AT',RF1=VB,LR1=084,                          
//*                                                                             
//           F2='W351.W351X3CH.W35111',                                         
//           T2='W351.W351V1.W35111CH',RF2=VB,LR2=084,                          
//*                                                                             
//           F3='W351.W351X3FI.W35111',                                         
//           T3='W351.W351V1.W35111FI',RF3=VB,LR3=084,                          
//*                                                                             
//           F4='W351.W351X3FR.W35111',                                         
//           T4='W351.W351V1.W35111FR',RF4=VB,LR4=084,                          
//*                                                                             
//           F5='W351.W351X3GB.W35111',                                         
//           T5='W351.W351V1.W35111GB',RF5=VB,LR5=084,                          
//*                                                                             
//           F6='W351.W351X3IT.W35111',                                         
//           T6='W351.W351V1.W35111IT',RF6=VB,LR6=084,                          
//*                                                                             
//           F7='W351.W351X3JP.W35111',                                         
//           T7='W351.W351V1.W35111JP',RF7=VB,LR7=084,                          
//*                                                                             
//           F8='W351.W351X3PL.W35111',                                         
//           T8='W351.W351V1.W35111PL',RF8=VB,LR8=084                           
//*                                                                             
//SPOC      EXEC PGM=CSLUSPOC,                                                  
//  PARM=('IMSPLEX=IMG0,ROUTE=*,WAIT=30')                                       
//SYSPRINT  DD SYSOUT=*                                                         
//SYSIN     DD *                                                                
 UPDATE DB NAME(WDK8*) STOP(ACCESS)                                             
//*                                                                             
//*                                                                             
//WAIT    EXEC WWAIT,SECONDS=10                                                 
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W351V1RS                                         
