//W561J076 JOB (640W5610100W561J076,W100),'RTN W561D3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W561    EXEC W561P076                                                         
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W561.W561D3.W56176A(+1)                              
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP2,DSIN=W561.W561D3.W56176A(+1)                                   
//SYSIN            DD *                                                         
W56176-001                                                                      
W56176                                                                          
//    ENDIF                                                                     
//SOPEND  EXEC WSOPEND,PROCESS=W561J076                                         
