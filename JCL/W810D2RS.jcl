//W810D2RS JOB (640W8100100W810D2RS,W100),'RTN W810D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W810D2                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//           F1='W111.W100V1.W11184A',                                          
//           T1='W111.W810D2.W11184A',RF1=FB,LR1=184,                           
//*                                                                             
//           F2='W611.W611D1.W61110A',                                          
//           T2='W611.W810D2.W61110A',RF2=FB,LR2=177,                           
//*                                                                             
//           F3='WXTR.WXTRD1.WXTRA0A',                                          
//           T3='WXTR.W810D2.WXTRA0A',RF3=FB,LR3=142,                           
//*                                                                             
//           F4='WXTR.WXTRV2.PACKRAD',                                          
//           T4='WXTR.W810D2.PACKRAD',RF4=FB,LR4=142,                           
//*                                                                             
//           F5='W412.W412S4.W41218',                                           
//           T5='W412.W810D2.W41218',RF5=VB,LR5=80                              
//*                                                                             
//COPY    EXEC PGM=ICEGENER                                                     
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W412.W810D2.W41218,DISP=SHR                                  
//SYSUT2   DD  DSN=WXTR.VORREP.DAILY(+1),                                       
//             DISP=(NEW,CATLG,DELETE),                                         
//             MGMTCLAS=BACKUPC,DATACLAS=PSEN                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W810D2RS                                         
