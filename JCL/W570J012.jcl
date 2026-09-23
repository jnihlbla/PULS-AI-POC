//W570J012 JOB (650W5700100W570J012,W100),'RTN W570D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTZ                                                     
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W570     EXEC W570P012                                                        
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W570.W570D1.W51710(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W570.W570D1.W51710(+1)                                    
//SYSIN           DD *                                                          
W51710-001                                                                      
W51710-CN                                                                       
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W570J012                                         
