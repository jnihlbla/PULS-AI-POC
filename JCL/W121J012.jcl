//W121J012 JOB (650W1210100W121J012,W100),'RTN W121Q1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W121    EXEC W121P012                                                         
//*                                                                             
//*  -- SKICKA FILEN MED MAIL GENOM "DISTRIBUTION AND PRINT"                    
// EXEC WZ14PDAP,DSIN=W121.W121Q1.W12113(+1)                                    
//SYSIN           DD *                                                          
W12112-001                                                                      
W1211200                                                                        
//SOP     EXEC WSOPEND,PROCESS=W121J012                                         
