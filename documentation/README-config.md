# GPS2 Docker Configuration

## Quick Setup

### macOS

```bash
cd docker-compose
cp .env.macos .env
docker-compose up -d
```

### Windows

```cmd
cd docker-compose
copy .env.windows .env
docker-compose up -d
```

## Configuration Files

All environment files are located in the `docker-compose/` directory:

- `.env.macos` - macOS configuration with `/Volumes/` mount paths
- `.env.windows` - Windows configuration with mapped drive paths
- `.env.example` - Template for custom configurations
- `.env` - Active configuration (ignored by git)

## Windows Dual Drive Setup

The Windows configuration supports UW-Madison's dual research drives:

### Pre-configured drives:
- **S: Drive**: `\\research.drive.wisc.edu\jjcurtin\studydata` → `/research_data` in container
- **R: Drive**: `\\restricted.drive.wisc.edu\jjcurtin` → `/restricted_data` in container

### Environment variables:
```bash
RESEARCH_DATA_PATH=S:/studydata/risk
RESTRICTED_DATA_PATH=R:/
```

### Verification:
```bash
# Test both drive mounts after startup
docker-compose exec -T postgis ls -l /research_data/
docker-compose exec -T postgis ls -l /restricted_data/
```

### Alternative paths:
If drive mapping doesn't work, edit `.env.windows` to use UNC paths:
```bash
RESEARCH_DATA_PATH=//research.drive.wisc.edu/jjcurtin/studydata/risk
RESTRICTED_DATA_PATH=//restricted.drive.wisc.edu/jjcurtin
```
