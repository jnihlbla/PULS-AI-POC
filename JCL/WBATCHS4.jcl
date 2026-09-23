//WBATCHS4 JOB (540W0090100WBATCHS4,W100),'RTN W980D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=5,FORMS=1800                                                    
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WAIT    EXEC WWAIT,SECONDS=300                                                
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WBATCHS4                                         
