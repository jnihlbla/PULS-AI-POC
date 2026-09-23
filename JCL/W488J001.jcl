//W488J001 JOB (650W4880100W488J001,W100),'RTN W488B1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0,LINES=9999                                        
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W488    EXEC W488P001                                                         
//SOP     EXEC WSOPEND,PROCESS=W488J001                                         
