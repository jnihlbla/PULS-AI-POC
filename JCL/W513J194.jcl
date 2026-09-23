//W513J194 JOB (640W5130100W513J194,W100),'RTN W513V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND VCC1                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//******************************************************************            
//* MAIL TILL EKONOMI/INVENTERING VIA D&P                                       
//* W513.W513V1.W51393, TOTAL STOCKVALUE                                        
//******************************************************************            
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W513.W513V1.W51393(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W513.W513V1.W51393(+0)                                    
//SYSIN           DD *                                                          
W51394-001                                                                      
W51394                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W513J194                                         
