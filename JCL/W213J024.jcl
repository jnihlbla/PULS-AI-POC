//W213J024 JOB (640W2130100W213J024,W100),'RTN W213P1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ NJEVC                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*                                                                             
//WZ14    EXEC WZ14DAP2,DSIN=W213.W213P1.W21324(+0),CPU=2                       
//SYSIN        DD *                                                             
W21323-001                                                                      
A                                                                               
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W213J024                                         
