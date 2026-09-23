//WZ11JABE JOB (650W0090100WZ11JABE,W100),'RTN WZ11S9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=9,FORMS=1800                                                    
/*ROUTE XEQ    LOCAL                                                            
/*ROUTE PRINT  LOCAL                                                            
//*                                                                             
//REP    EXEC  WMAILSND                                                         
)SEND                                                                           
TITLE Abend in a D & P job in QASE                                              
DEST wsyst@volvocars.com                                                        
MAIL                                                                            
                                                                                
 A Distribution & Print job &ACTION to                                          
 &DEST has failed.                                                              
 Date and approximate time for this job was: &TIMESTAMP                         
 For more details, see job &JOBNAME in SAR                                      
                                                                                
 &INFO                                                                          
   &REGDAT                                                                      
   &KLOCK                                                                       
   &TYPE                                                                        
   &REC                                                                         
   &LIST                                                                        
)END                                                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WZ11JABE                                         
