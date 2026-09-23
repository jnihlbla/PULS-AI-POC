//W488S2OK JOB (540W4880100W488S2OK,W100),'RTN W488S2',                         
//*            USER=RTSO002,PASSWORD=RTSO002,                                   
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP,COMMAND='ORDER W488S2'                                      
//*                                                                             
//ABE     IF ABEND THEN                                                         
//SOP     EXEC WSOP,COMMAND='ABEND W488S2'                                      
//ABE     ENDIF                                                                 
