//W015J127 JOB (670W5110100W015J127,W100),'RTN W015D2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=WXTR.W015D2.W01527D(+0)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=WXTR.W015D2.W01527D(+0)                                   
//SYSIN           DD *                                                          
W01527-001                                                                      
W01527                                                                          
//*                                                                             
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W015J127                                         
