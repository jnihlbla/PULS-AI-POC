//W479JD83 JOB (640W4790100W479JD83,W100),'RTN W479D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
// EXEC WZ14PDAP,DSIN=W479.W479D1.W47983(+0)                                    
//SYSIN           DD *                                                          
W47983-001                                                                      
W47983                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W479JD83                                         
