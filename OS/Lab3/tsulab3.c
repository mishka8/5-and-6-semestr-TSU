#include <linux/init.h>
#include <linux/kernel.h>  
#include <linux/module.h>
#include <linux/proc_fs.h>
#include <linux/uaccess.h>
#include <linux/version.h>
#include <linux/time.h>
#include <linux/ktime.h>
#include <linux/string.h>
#include <linux/math64.h>

//Расчитайте, когда Солнце покинули лучи света, которыми мы сейчас видим отражёнными от Луны

#define procfs_name "TSULab"

static struct proc_dir_entry *our_proc_file = NULL;

static ssize_t procfile_read(struct file *filePointer, char __user *buffer, size_t buffer_length, loff_t *offset)
{
    struct timespec64 current_time;
    char s[64];
    ssize_t strLen;

    ktime_get_real_ts64(&current_time);
    
    u32 minutes;
    u32 seconds;

    u64 earth_moon_distance_m = 384400000ULL;
    u64 speed_of_light_mps = 299792458ULL;
    u64 sun_earth_distance_m = 149597870700ULL;

    u64 time_moon_to_earth = div64_u64(earth_moon_distance_m, speed_of_light_mps);
    u64 time_sun_to_earth = div64_u64(sun_earth_distance_m, speed_of_light_mps);
    
    u64 total_travel_time = time_sun_to_earth + time_moon_to_earth; 
    
    struct timespec64 sun_departure_time = current_time;
    
    if (sun_departure_time.tv_sec >= total_travel_time) 
    {
        sun_departure_time.tv_sec -= total_travel_time;
    } 
    else 
    {
        sun_departure_time.tv_sec = 0;
    }
    
    minutes = total_travel_time / 60;
    seconds = total_travel_time % 60;

    snprintf(s, sizeof(s), "Total time: %u min %u sec ago\n", minutes, seconds);
    
    strLen = strlen(s);


    
    if (*offset >= strLen || copy_to_user(buffer, s, strLen)) 
    {
        pr_info("copy_to_user failed\n");
        return 0;
    }

    pr_info("procfile read %s\n", filePointer->f_path.dentry->d_name.name);
    *offset += strLen;

    return strLen;
}

#if LINUX_VERSION_CODE >= KERNEL_VERSION(5, 6, 0)

static const struct proc_ops proc_file_fops = {
    .proc_read = procfile_read,
};

#else

static const struct file_operations proc_file_fops = {
    .read = procfile_read,
};

#endif

static int __init procFsInit(void)
{
    our_proc_file = proc_create (procfs_name, 0644, NULL, &proc_file_fops);

    if (NULL == our_proc_file) 
    {
        proc_remove(our_proc_file);
        pr_alert("Error: could not initialize /proc/%s\n", procfs_name);

        return -ENOMEM;
    }
    
    pr_info("Welcome to Tomsk State University\n");
    pr_info("/proc/%s was created\n", procfs_name);

    return 0;
}

static void __exit procFsExit(void)
{
    proc_remove(our_proc_file);
    pr_info("/proc/%s was removed\n", procfs_name);
    pr_info("Tomsk State University forever!\n");
}

module_init(procFsInit);
module_exit(procFsExit);


MODULE_LICENSE("GPL");

MODULE_AUTHOR("Pronin Leonid Lab3");


