//WBATCHY1 JOB (540W0090100WBATCHY1,W100),'RTN W980D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=5,FORMS=1800                                                    
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
START WBATCHZ1                                                                  
//*                                                                             
//  IF (RC >= 8) THEN                                                           
//ABEND   EXEC VRCABEND                                                         
//  ENDIF                                                                       
//*                                                                             
//END     EXEC WSOPEND,PROCESS=WBATCHY1                                         
