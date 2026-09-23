//W430ARD1 JOB (670W4300100W430ARD1,W100),'RTN W430V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//DUMP     EXEC WG02DUMP,                                                       
//             UID=W430ARD1,                                                    
//             DSOUT=W430.DUMP.SP6ARTP(+1)                                      
COPY TABLESPACE DWL10.SP6ARTP                                                   
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=W430ARD1                                         
//*                                                                             
