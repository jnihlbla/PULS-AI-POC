//W570J073 JOB (650W5700100W570J073,W100),'RTN W570D3',                         
//             USER=?,PASSWORD=?,CLASS=V                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W570    EXEC W570P073                                                         
//*                                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W570.W570D3.W5707Q(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP2,DSIN=W570.W570D3.W5707Q(+1)                                    
//SYSIN            DD *                                                         
W57073-003                                                                      
W57073                                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W570.W570D3.W5707R(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP2,DSIN=W570.W570D3.W5707R(+1)                                    
//SYSIN            DD *                                                         
W57073-004                                                                      
W57073                                                                          
//    ENDIF                                                                     
//SOP     EXEC WSOPEND,PROCESS=W570J073                                         
