//W479J088 JOB (640W4790100W479J088,W100),'RTN W479D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM FORMS=1800,LINECT=00                                                  
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W479    EXEC W479P088                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W479J088                                         
