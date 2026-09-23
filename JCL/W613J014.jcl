//W613J014 JOB (640W6130100W613J014,W100),'RTN W613V2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST6                                                    
//      INCLUDE MEMBER=SYSTZ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W613    EXEC W613P014                                                         
//*                                                                             
//********************************************************************          
//* MAIL TILL VCCS VIA D&P                                                      
//* W613.W613V2.W61315(+1), FÖRPACKAT SVS                                       
//********************************************************************          
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W613.W613V2.W61315(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W613.W613V2.W61315(+1)                                    
//SYSIN           DD *                                                          
W61314-001                                                                      
W61314                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W613J014                                         
