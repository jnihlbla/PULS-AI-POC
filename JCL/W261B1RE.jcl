//W261B1RE JOB (650W2610100W261B1RE,W100),'RTN W261B1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE  XEQ  LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W261B1,MAXRC=8                                        
//SOP     EXEC WSOPEND,PROCESS=W261B1RE                                         
/*                                                                              
