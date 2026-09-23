//W980J045   JOB (540W0090100W980J045,W100),'RTN W980R2',                       
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800                                                            
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W980    EXEC W980P045                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W980J045                                         
