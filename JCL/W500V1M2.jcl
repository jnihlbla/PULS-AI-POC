//W500V1M2 JOB (640W5100100W500V1M2,W100),'RTN W500V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//********************************************************************          
//*                                                                  *          
//* MEMO TILL EKONOMIAVD OM VECKANS IN- OCH UT-GÅENDE LAGV O SAPBOKN *          
//* W510.W500V1.W51096(0),DISP=SHR                                   *          
//********************************************************************          
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W510.W500V1.W51096(+0)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W510.W500V1.W51096(+0)                                    
//SYSIN           DD *                                                          
W51095-001                                                                      
W51095                                                                          
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W500V1M2                                         
