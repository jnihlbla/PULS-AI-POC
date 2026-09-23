//W561J073 JOB (650W5100100W561J073,W100),'RTN W561D3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST5                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W561    EXEC W561P073                                                         
//*                                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W561.W561D3.W5617Q(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//WZ14   EXEC WZ14DAP2,DSIN=W561.W561D3.W5617Q(+1)                              
//SYSIN            DD *                                                         
W56173-003                                                                      
W56173                                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W561.W561D3.W5617R(+1)                               
//    IF (EMPTY2.T.RC = 0) THEN                                                 
//WZ14   EXEC WZ14DAP2,DSIN=W561.W561D3.W5617R(+1)                              
//SYSIN            DD *                                                         
W56173-004                                                                      
W56173                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W561J073                                         
