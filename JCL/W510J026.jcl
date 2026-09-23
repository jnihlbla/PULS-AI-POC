//W510J026 JOB (650W5100100W510J026,W100),'RTN W510Y1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM LINES=99,FORMS=1800,LINECT=0                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W510    EXEC W510P026                                                         
//W51026.DBOCTRL   DD *                                                         
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:43:47                             
                                                                                
  FUNCTION=RELOAD,USERLOAD,                                                     
  PCB=1,                                                                        
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//DBRC    EXEC WG01DBRC                                                         
//DBRC.SYSIN DD *                                                               
NOTIFY.UIC  DBD(WDK1) DDN(WDK1V) UDATA('VID RESTORE KÖR OM')                    
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W510J026                                         
//*                                                                             
