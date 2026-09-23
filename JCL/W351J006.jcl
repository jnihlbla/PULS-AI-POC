//W351J006 JOB (640W3510100W351J006,W100),'RTN W351V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM LINES=99,FORMS=1800,LINECT=0                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//AMS     EXEC PGM=IDCAMS                                                       
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DSN=W.QASE.CONSTANT(WDK8CLU),DISP=SHR                            
//*                                                                             
//W351    EXEC W351P006                                                         
//W35106.DBOCTRL   DD *                                                         
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:43:21                             
                                                                                
  FUNCTION=RELOAD,USERLOAD,                                                     
  PCB=1,                                                                        
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//DBRC    EXEC WG01DBRC                                                         
//DBRC.SYSIN DD *                                                               
NOTIFY.UIC  DBD(WDK8) DDN(WDK8K) UDATA('VID RESTORE KÖR OM')                    
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W351J006                                         
//*                                                                             
