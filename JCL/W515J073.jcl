//W515J073 JOB (650W5100100W515J073,W100),'RTN W515D3',                         
//             USER=?,PASSWORD=?,CLASS=V                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W515    EXEC W515P073                                                         
//*                                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W515.W515D3.W5157Q(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//WZ14   EXEC WZ14DAP2,DSIN=W515.W515D3.W5157Q(+1)                              
//SYSIN            DD *                                                         
W51573-003                                                                      
W51573                                                                          
//    ENDIF                                                                     
//*                                                                             
//EMPTY2 EXEC WEMPTST,DSIN=W515.W515D3.W5157R(+1)                               
//    IF (EMPTY2.T.RC = 0) THEN                                                 
//WZ14   EXEC WZ14DAP2,DSIN=W515.W515D3.W5157R(+1)                              
//SYSIN            DD *                                                         
W51573-004                                                                      
W51573                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W515J073                                         
