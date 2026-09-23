//W092J108 JOB (650W0920100W092J108,W100),'RTN W092D6',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//*               //*---  WMEMOSND,EXC                                          
//W092    EXEC W092P108                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W092J108                                         
//*                                                                             
//TOMDEL   EXEC WEMPDEL,DSIN=W092.W092D6.W09299(+1)                             
//*                                                                             
//WMEMOSND EXEC WMEMOSND,CONDS='(0,LT,TOMDEL.T)'                                
)SEND                                                                           
TITLE W09299-FIL-FEL                                                            
DEST WSYST@VOLVOCARS.COM                                                        
OPTION FORCE                                                                    
MEMO                                                                            
                                                                                
 FELAKTIGA TRANSAKTIONS-TYPER UR W09208-PROGRAMMET.                             
 SE FIL W092.W092D6.W09299                                                      
                                                                                
)END                                                                            
