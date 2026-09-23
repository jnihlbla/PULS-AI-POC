//W488X2SE JOB (540W4880100W488X2SE,W100),'RTN W488X2',                         
//             CLASS=K                                                          
/*JOBPARM TIME=1,LINES=5,FORMS=1800,LINECT=0                                    
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOP,COMMAND='ORDER W488X2'                                      
