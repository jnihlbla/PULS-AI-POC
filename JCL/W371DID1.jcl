//W371DID1 JOB (650W3710100W371DID1,W100),'RTN W371V9',                         
//             CLASS=K,                                                         
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//DUMP     EXEC WG02DUMP,                                                       
//             UID=W371DID1,                                                    
//             DSOUT=WG02.DUMP.W371DI(+1)                                       
COPY TABLESPACE DW371.SBYLDIST                                                  
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=W371DID1                                         
//*                                                                             
