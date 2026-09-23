//W981JSTS JOB (650W0090100W981JSTS,W100),'RTN W981D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.PROD.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVPROD                                                   
/*JOBPARM LINES=50,FORMS=1800                                                   
//*                        *AFTER W981JLST                                      
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                        *CNTL  WSOPDAT,EXC                                   
//*                                                                             
//SOP1    EXEC WSOP                                                             
ACTIVATE WSATS                                                                  
//*                                                                             
//  IF (RC >= 8) THEN                                                           
//ABEND   EXEC VRCABEND                                                         
//  ENDIF                                                                       
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W981JSTS                                         
